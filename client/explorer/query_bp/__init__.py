from flask import request, Response, render_template, Blueprint, jsonify
from explorer.utils import utils
from explorer.queries import query
from pprint import pprint

# Blueprint Configuration
query_bp = Blueprint(
    'query_bp', __name__,
    template_folder='templates',
)

@query_bp.get("/")
def index_get():
    return render_template('query.html')

@query_bp.post("/")
def index_post():
    result = query.do_query(request.form['query'])
    pprint(result)
    return jsonify(result)
