from flask import request, render_template, Blueprint, jsonify, abort
from explorer.queries import query

# Blueprint Configuration
query_bp = Blueprint(
    'query_bp', __name__,
    template_folder='templates',
)

@query_bp.errorhandler(400)
def bad_request(e):
    # note that we set the 500 status explicitly
    return render_template('query_bp/query.html', data={"error": e}), 400

@query_bp.get("/")
def index_get():
    default_query = """g.V()
.filter{it.get().value('pname').matches('auto-multiple-choice')}
.repeat(outE().otherV().simplePath())
.until(__.not(outE().simplePath()))
.path()
.by('pname')
.by(label)
    """
    return render_template('query_bp/query.html', data={"default_query": default_query})

@query_bp.post("/")
def index_post():
    try:
        GR = query.GremlinResult(request.form['query'], clean_gremlin=True)
        result = GR.to_dict()
        match result:
            case None:
                raise Exception("Could not get Gremlin result from server.")
            case _:
                return jsonify(result)
    except Exception as e:
        return jsonify({"error": str(e)})
