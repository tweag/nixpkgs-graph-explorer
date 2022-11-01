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
    return render_template('query_bp/query.html')

@query_bp.post("/")
def index_post():
    try:
        GR = query.GremlinResult(request.form['query'])
        result = GR.to_dict()
        match result:
            case None:
                raise Exception("fail")
            case _:
                return jsonify(result)
    except:
        return jsonify({"error": "fail"})
